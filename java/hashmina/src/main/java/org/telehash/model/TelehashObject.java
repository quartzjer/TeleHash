/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package org.telehash.model;

import org.eclipse.emf.json.model.JsObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Object</b></em>'.
 * <!-- end-user-doc -->
 *
 *
 * @see org.telehash.model.TelehashPackage#getTelehashObject()
 * @model
 * @generated
 */
public interface TelehashObject extends JsObject {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model dataType="org.eclipse.emf.json.model.AnySimpleType"
	 * @generated
	 */
	Object getHeader(String key);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model dataType="org.eclipse.emf.json.model.AnySimpleType"
	 * @generated
	 */
	Object getCommand(String key);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model dataType="org.eclipse.emf.json.model.AnySimpleType"
	 * @generated
	 */
	Object getSignal(String key);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model valueDataType="org.eclipse.emf.json.model.AnySimpleType"
	 * @generated
	 */
	TelehashObject withHeader(String key, Object value);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model valueDataType="org.eclipse.emf.json.model.AnySimpleType"
	 * @generated
	 */
	TelehashObject withCommand(String key, Object value);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model valueDataType="org.eclipse.emf.json.model.AnySimpleType"
	 * @generated
	 */
	TelehashObject withSignal(String key, Object value);

} // TelehashObject
