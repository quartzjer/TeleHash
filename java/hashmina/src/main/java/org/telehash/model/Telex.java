/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package org.telehash.model;

import java.net.InetSocketAddress;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.json.model.JsObject;
import org.telehash.Hash;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Telex</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.telehash.model.Telex#getTo <em>To</em>}</li>
 *   <li>{@link org.telehash.model.Telex#getEnd <em>End</em>}</li>
 *   <li>{@link org.telehash.model.Telex#getLine <em>Line</em>}</li>
 *   <li>{@link org.telehash.model.Telex#getRing <em>Ring</em>}</li>
 *   <li>{@link org.telehash.model.Telex#getSee <em>See</em>}</li>
 *   <li>{@link org.telehash.model.Telex#getBytesReceived <em>Bytes Received</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.telehash.model.TelehashPackage#getTelex()
 * @model
 * @generated
 */
public interface Telex extends JsObject {
	/**
	 * Returns the value of the '<em><b>To</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>To</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>To</em>' attribute.
	 * @see #setTo(InetSocketAddress)
	 * @see org.telehash.model.TelehashPackage#getTelex_To()
	 * @model dataType="org.telehash.model.Endpoint"
	 *        annotation="JsonMetadata keyType='header'"
	 * @generated
	 */
	InetSocketAddress getTo();

	/**
	 * Sets the value of the '{@link org.telehash.model.Telex#getTo <em>To</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>To</em>' attribute.
	 * @see #getTo()
	 * @generated
	 */
	void setTo(InetSocketAddress value);

	/**
	 * Returns the value of the '<em><b>End</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>End</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>End</em>' attribute.
	 * @see #setEnd(Hash)
	 * @see org.telehash.model.TelehashPackage#getTelex_End()
	 * @model dataType="org.telehash.model.Hash"
	 *        annotation="JsonMetadata keyType='signal'"
	 * @generated
	 */
	Hash getEnd();

	/**
	 * Sets the value of the '{@link org.telehash.model.Telex#getEnd <em>End</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>End</em>' attribute.
	 * @see #getEnd()
	 * @generated
	 */
	void setEnd(Hash value);

	/**
	 * Returns the value of the '<em><b>Line</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Line</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Line</em>' attribute.
	 * @see #isSetLine()
	 * @see #unsetLine()
	 * @see #setLine(int)
	 * @see org.telehash.model.TelehashPackage#getTelex_Line()
	 * @model unsettable="true"
	 *        annotation="JsonMetadata keyType='header'"
	 * @generated
	 */
	int getLine();

	/**
	 * Sets the value of the '{@link org.telehash.model.Telex#getLine <em>Line</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Line</em>' attribute.
	 * @see #isSetLine()
	 * @see #unsetLine()
	 * @see #getLine()
	 * @generated
	 */
	void setLine(int value);

	/**
	 * Unsets the value of the '{@link org.telehash.model.Telex#getLine <em>Line</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetLine()
	 * @see #getLine()
	 * @see #setLine(int)
	 * @generated
	 */
	void unsetLine();

	/**
	 * Returns whether the value of the '{@link org.telehash.model.Telex#getLine <em>Line</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Line</em>' attribute is set.
	 * @see #unsetLine()
	 * @see #getLine()
	 * @see #setLine(int)
	 * @generated
	 */
	boolean isSetLine();

	/**
	 * Returns the value of the '<em><b>Ring</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Ring</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Ring</em>' attribute.
	 * @see #isSetRing()
	 * @see #unsetRing()
	 * @see #setRing(int)
	 * @see org.telehash.model.TelehashPackage#getTelex_Ring()
	 * @model unsettable="true"
	 *        annotation="JsonMetadata keyType='header'"
	 * @generated
	 */
	int getRing();

	/**
	 * Sets the value of the '{@link org.telehash.model.Telex#getRing <em>Ring</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Ring</em>' attribute.
	 * @see #isSetRing()
	 * @see #unsetRing()
	 * @see #getRing()
	 * @generated
	 */
	void setRing(int value);

	/**
	 * Unsets the value of the '{@link org.telehash.model.Telex#getRing <em>Ring</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetRing()
	 * @see #getRing()
	 * @see #setRing(int)
	 * @generated
	 */
	void unsetRing();

	/**
	 * Returns whether the value of the '{@link org.telehash.model.Telex#getRing <em>Ring</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Ring</em>' attribute is set.
	 * @see #unsetRing()
	 * @see #getRing()
	 * @see #setRing(int)
	 * @generated
	 */
	boolean isSetRing();

	/**
	 * Returns the value of the '<em><b>See</b></em>' attribute list.
	 * The list contents are of type {@link java.net.InetSocketAddress}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>See</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>See</em>' attribute list.
	 * @see org.telehash.model.TelehashPackage#getTelex_See()
	 * @model dataType="org.telehash.model.Endpoint"
	 *        annotation="JsonMetadata keyType='command'"
	 * @generated
	 */
	EList<InetSocketAddress> getSee();

	/**
	 * Returns the value of the '<em><b>Bytes Received</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Bytes Received</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Bytes Received</em>' attribute.
	 * @see #setBytesReceived(int)
	 * @see org.telehash.model.TelehashPackage#getTelex_BytesReceived()
	 * @model annotation="JsonMetadata keyType='header' key='_br'"
	 * @generated
	 */
	int getBytesReceived();

	/**
	 * Sets the value of the '{@link org.telehash.model.Telex#getBytesReceived <em>Bytes Received</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Bytes Received</em>' attribute.
	 * @see #getBytesReceived()
	 * @generated
	 */
	void setBytesReceived(int value);

} // Telex
