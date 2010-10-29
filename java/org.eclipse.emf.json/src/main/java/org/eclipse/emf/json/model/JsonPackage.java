/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package org.eclipse.emf.json.model;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see org.eclipse.emf.json.model.JsonFactory
 * @model kind="package"
 * @generated
 */
public interface JsonPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "json";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://telehash.org/json/2010";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "json";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	JsonPackage eINSTANCE = org.eclipse.emf.json.model.impl.JsonPackageImpl.init();

	/**
	 * The meta object id for the '{@link org.eclipse.emf.json.model.impl.EStringToAnySimpleTypeMapImpl <em>EString To Any Simple Type Map</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.emf.json.model.impl.EStringToAnySimpleTypeMapImpl
	 * @see org.eclipse.emf.json.model.impl.JsonPackageImpl#getEStringToAnySimpleTypeMap()
	 * @generated
	 */
	int ESTRING_TO_ANY_SIMPLE_TYPE_MAP = 0;

	/**
	 * The feature id for the '<em><b>Key</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ESTRING_TO_ANY_SIMPLE_TYPE_MAP__KEY = 0;

	/**
	 * The feature id for the '<em><b>Value</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ESTRING_TO_ANY_SIMPLE_TYPE_MAP__VALUE = 1;

	/**
	 * The number of structural features of the '<em>EString To Any Simple Type Map</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ESTRING_TO_ANY_SIMPLE_TYPE_MAP_FEATURE_COUNT = 2;

	/**
	 * The meta object id for the '{@link org.eclipse.emf.json.model.impl.JsObjectImpl <em>Js Object</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.emf.json.model.impl.JsObjectImpl
	 * @see org.eclipse.emf.json.model.impl.JsonPackageImpl#getJsObject()
	 * @generated
	 */
	int JS_OBJECT = 1;

	/**
	 * The feature id for the '<em><b>Unmatched</b></em>' map.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int JS_OBJECT__UNMATCHED = 0;

	/**
	 * The number of structural features of the '<em>Js Object</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int JS_OBJECT_FEATURE_COUNT = 1;

	/**
	 * The meta object id for the '<em>Any Simple Type</em>' data type.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see java.lang.Object
	 * @see org.eclipse.emf.json.model.impl.JsonPackageImpl#getAnySimpleType()
	 * @generated
	 */
	int ANY_SIMPLE_TYPE = 2;


	/**
	 * Returns the meta object for class '{@link java.util.Map.Entry <em>EString To Any Simple Type Map</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>EString To Any Simple Type Map</em>'.
	 * @see java.util.Map.Entry
	 * @model keyDataType="org.eclipse.emf.ecore.EString"
	 *        valueDataType="org.eclipse.emf.json.model.AnySimpleType"
	 * @generated
	 */
	EClass getEStringToAnySimpleTypeMap();

	/**
	 * Returns the meta object for the attribute '{@link java.util.Map.Entry <em>Key</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Key</em>'.
	 * @see java.util.Map.Entry
	 * @see #getEStringToAnySimpleTypeMap()
	 * @generated
	 */
	EAttribute getEStringToAnySimpleTypeMap_Key();

	/**
	 * Returns the meta object for the attribute '{@link java.util.Map.Entry <em>Value</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Value</em>'.
	 * @see java.util.Map.Entry
	 * @see #getEStringToAnySimpleTypeMap()
	 * @generated
	 */
	EAttribute getEStringToAnySimpleTypeMap_Value();

	/**
	 * Returns the meta object for class '{@link org.eclipse.emf.json.model.JsObject <em>Js Object</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Js Object</em>'.
	 * @see org.eclipse.emf.json.model.JsObject
	 * @generated
	 */
	EClass getJsObject();

	/**
	 * Returns the meta object for the map '{@link org.eclipse.emf.json.model.JsObject#getUnmatched <em>Unmatched</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the map '<em>Unmatched</em>'.
	 * @see org.eclipse.emf.json.model.JsObject#getUnmatched()
	 * @see #getJsObject()
	 * @generated
	 */
	EReference getJsObject_Unmatched();

	/**
	 * Returns the meta object for data type '{@link java.lang.Object <em>Any Simple Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for data type '<em>Any Simple Type</em>'.
	 * @see java.lang.Object
	 * @model instanceClass="java.lang.Object"
	 * @generated
	 */
	EDataType getAnySimpleType();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	JsonFactory getJsonFactory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 *   <li>each class,</li>
	 *   <li>each feature of each class,</li>
	 *   <li>each enum,</li>
	 *   <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link org.eclipse.emf.json.model.impl.EStringToAnySimpleTypeMapImpl <em>EString To Any Simple Type Map</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.emf.json.model.impl.EStringToAnySimpleTypeMapImpl
		 * @see org.eclipse.emf.json.model.impl.JsonPackageImpl#getEStringToAnySimpleTypeMap()
		 * @generated
		 */
		EClass ESTRING_TO_ANY_SIMPLE_TYPE_MAP = eINSTANCE.getEStringToAnySimpleTypeMap();

		/**
		 * The meta object literal for the '<em><b>Key</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ESTRING_TO_ANY_SIMPLE_TYPE_MAP__KEY = eINSTANCE.getEStringToAnySimpleTypeMap_Key();

		/**
		 * The meta object literal for the '<em><b>Value</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ESTRING_TO_ANY_SIMPLE_TYPE_MAP__VALUE = eINSTANCE.getEStringToAnySimpleTypeMap_Value();

		/**
		 * The meta object literal for the '{@link org.eclipse.emf.json.model.impl.JsObjectImpl <em>Js Object</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.emf.json.model.impl.JsObjectImpl
		 * @see org.eclipse.emf.json.model.impl.JsonPackageImpl#getJsObject()
		 * @generated
		 */
		EClass JS_OBJECT = eINSTANCE.getJsObject();

		/**
		 * The meta object literal for the '<em><b>Unmatched</b></em>' map feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference JS_OBJECT__UNMATCHED = eINSTANCE.getJsObject_Unmatched();

		/**
		 * The meta object literal for the '<em>Any Simple Type</em>' data type.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see java.lang.Object
		 * @see org.eclipse.emf.json.model.impl.JsonPackageImpl#getAnySimpleType()
		 * @generated
		 */
		EDataType ANY_SIMPLE_TYPE = eINSTANCE.getAnySimpleType();

	}

} //JsonPackage
