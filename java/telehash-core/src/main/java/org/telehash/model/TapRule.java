/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package org.telehash.model;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.json.model.JsObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Tap Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.telehash.model.TapRule#getIs <em>Is</em>}</li>
 *   <li>{@link org.telehash.model.TapRule#getHas <em>Has</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.telehash.model.TelehashPackage#getTapRule()
 * @model
 * @generated
 */
public interface TapRule extends JsObject {
	/**
	 * Returns the value of the '<em><b>Is</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Is</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Is</em>' containment reference.
	 * @see #isSetIs()
	 * @see #unsetIs()
	 * @see #setIs(JsObject)
	 * @see org.telehash.model.TelehashPackage#getTapRule_Is()
	 * @model containment="true" unsettable="true"
	 * @generated
	 */
	JsObject getIs();

	/**
	 * Sets the value of the '{@link org.telehash.model.TapRule#getIs <em>Is</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Is</em>' containment reference.
	 * @see #isSetIs()
	 * @see #unsetIs()
	 * @see #getIs()
	 * @generated
	 */
	TapRule withIs(JsObject value);

	void setIs(JsObject value);

	/**
	 * Unsets the value of the '{@link org.telehash.model.TapRule#getIs <em>Is</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetIs()
	 * @see #getIs()
	 * @see #setIs(JsObject)
	 * @generated
	 */
	void unsetIs();

	/**
	 * Returns whether the value of the '{@link org.telehash.model.TapRule#getIs <em>Is</em>}' containment reference is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Is</em>' containment reference is set.
	 * @see #unsetIs()
	 * @see #getIs()
	 * @see #setIs(JsObject)
	 * @generated
	 */
	boolean isSetIs();

	/**
	 * Returns the value of the '<em><b>Has</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Has</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Has</em>' attribute list.
	 * @see #isSetHas()
	 * @see #unsetHas()
	 * @see org.telehash.model.TelehashPackage#getTapRule_Has()
	 * @model unsettable="true"
	 * @generated
	 */
	EList<String> getHas();

	/**
	 * Unsets the value of the '{@link org.telehash.model.TapRule#getHas <em>Has</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetHas()
	 * @see #getHas()
	 * @generated
	 */
	void unsetHas();

	/**
	 * Returns whether the value of the '{@link org.telehash.model.TapRule#getHas <em>Has</em>}' attribute list is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Has</em>' attribute list is set.
	 * @see #unsetHas()
	 * @see #getHas()
	 * @generated
	 */
	boolean isSetHas();

} // TapRule
